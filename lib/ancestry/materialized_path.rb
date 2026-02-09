# frozen_string_literal: true

module Ancestry
  # store ancestry as grandparent_id/parent_id
  # root a=nil,id=1   children=id,id/%      == 1, 1/%
  # 3: a=1/2,id=3     children=a/id,a/id/%  == 1/2/3, 1/2/3/%
  module MaterializedPath
    def self.extended(base)
      base.send(:include, InstanceMethods)
    end

    def path_of(object)
      inpath_of(object)
    end

    def roots
      where(arel_table[ancestry_column].eq(ancestry_root))
    end

    # Convert object to a scope suitable for subqueries.
    # Accepts: ActiveRecord::Relation, record, array of records/IDs, or a single ID.
    def ancestor_scope(object)
      if object.is_a?(ActiveRecord::Relation)
        object
      elsif object.is_a?(ancestry_base_class)
        unscoped_where { |scope| scope.where(primary_key => object.id) }
      elsif object.is_a?(Array)
        ids = object.map { |o| o.is_a?(ancestry_base_class) ? o.id : o }
        unscoped_where { |scope| scope.where(primary_key => ids) }
      else
        unscoped_where { |scope| scope.where(primary_key => object) }
      end
    end

    def ancestors_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        t = arel_table
        return where(t[primary_key].in(object.ancestor_ids))
      end

      ca_sql = child_ancestry_sql
      joins(
        "INNER JOIN (#{ancestor_scope(object).select(ancestry_column).to_sql}) ancestors_src" \
        " ON ancestors_src.#{ancestry_column} IS NOT NULL" \
        " AND (ancestors_src.#{ancestry_column} = #{ca_sql}" \
        " OR ancestors_src.#{ancestry_column} LIKE #{concat(ca_sql, "'#{ancestry_delimiter}%'")})"
      )
    end

    def inpath_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        t = arel_table
        return where(t[primary_key].in(object.path_ids))
      end

      ca_sql = child_ancestry_sql
      sub = ancestor_scope(object).select("#{primary_key} AS #{primary_key}, #{ancestry_column}, #{child_ancestry_sql} AS child_ancestry").to_sql
      joins(
        "INNER JOIN (#{sub}) inpath_src" \
        " ON (#{table_name}.#{primary_key} = inpath_src.#{primary_key}" \
        " OR (inpath_src.#{ancestry_column} IS NOT NULL" \
        " AND (inpath_src.#{ancestry_column} = #{ca_sql}" \
        " OR inpath_src.#{ancestry_column} LIKE #{concat(ca_sql, "'#{ancestry_delimiter}%'")})))"
      )
    end

    def children_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        where(ancestry_column => object.child_ancestry)
      else
        where(ancestry_column => ancestor_scope(object).select(Arel.sql(child_ancestry_sql)))
      end
    end

    # indirect = anyone who is a descendant, but not a child
    def indirects_of(object)
      sub = ancestor_scope(object).select("#{child_ancestry_sql} AS child_ancestry").to_sql
      joins(
        "INNER JOIN (#{sub}) indirects_src" \
        " ON #{table_name}.#{ancestry_column} LIKE #{concat("indirects_src.child_ancestry", "'#{ancestry_delimiter}%'")}"
      )
    end

    def descendants_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        return where(descendant_conditions(object))
      end

      sub = ancestor_scope(object).select("#{child_ancestry_sql} AS child_ancestry").to_sql
      joins(
        "INNER JOIN (#{sub}) descendants_src" \
        " ON (#{table_name}.#{ancestry_column} = descendants_src.child_ancestry" \
        " OR #{table_name}.#{ancestry_column} LIKE #{concat("descendants_src.child_ancestry", "'#{ancestry_delimiter}%'")})"
      )
    end

    def descendants_by_ancestry(ancestry)
      t = arel_table
      t[ancestry_column].matches("#{ancestry}#{ancestry_delimiter}%", nil, true).or(t[ancestry_column].eq(ancestry))
    end

    def descendant_conditions(object)
      descendants_by_ancestry(object.child_ancestry)
    end

    def descendant_before_last_save_conditions(object)
      descendants_by_ancestry(object.child_ancestry_before_last_save)
    end

    def subtree_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        return descendants_of(object).or(where(primary_key => object.id))
      end

      sub = ancestor_scope(object).select("#{primary_key} AS #{primary_key}, #{child_ancestry_sql} AS child_ancestry").to_sql
      joins(
        "INNER JOIN (#{sub}) subtree_src" \
        " ON (#{table_name}.#{primary_key} = subtree_src.#{primary_key}" \
        " OR #{table_name}.#{ancestry_column} = subtree_src.child_ancestry" \
        " OR #{table_name}.#{ancestry_column} LIKE #{concat("subtree_src.child_ancestry", "'#{ancestry_delimiter}%'")})"
      )
    end

    def siblings_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        where(ancestry_column => object.read_attribute(ancestry_column))
      else
        where(ancestry_column => ancestor_scope(object).select(Arel.sql(ancestry_sql)))
      end
    end

    def ordered_by_ancestry(order = nil)
      if %w(mysql mysql2 sqlite sqlite3).include?(connection.adapter_name.downcase)
        reorder(arel_table[ancestry_column], order)
      elsif %w(postgresql oracleenhanced).include?(connection.adapter_name.downcase) && ActiveRecord::VERSION::STRING >= "6.1"
        reorder(Arel::Nodes::Ascending.new(arel_table[ancestry_column]).nulls_first, order)
      else
        reorder(
          Arel::Nodes::Ascending.new(Arel::Nodes::NamedFunction.new('COALESCE', [arel_table[ancestry_column], Arel.sql("''")])),
          order
        )
      end
    end

    def ordered_by_ancestry_and(order)
      ordered_by_ancestry(order)
    end

    def ancestry_root
      nil
    end

    def ancestry_sql(tbl = table_name)
      "#{tbl}.#{ancestry_column}"
    end

    def child_ancestry_sql(tbl = table_name)
      %{
        CASE WHEN #{tbl}.#{ancestry_column} IS NULL THEN #{concat("#{tbl}.#{primary_key}")}
        ELSE      #{concat("#{tbl}.#{ancestry_column}", "'#{ancestry_delimiter}'", "#{tbl}.#{primary_key}")}
        END
      }
    end

    def ancestry_depth_sql
      @ancestry_depth_sql ||= MaterializedPath.construct_depth_sql(table_name, ancestry_column, ancestry_delimiter)
    end

    def generate_ancestry(ancestor_ids)
      if ancestor_ids.present? && ancestor_ids.any?
        ancestor_ids.join(ancestry_delimiter)
      else
        ancestry_root
      end
    end

    def parse_ancestry_column(obj)
      return [] if obj.nil? || obj == ancestry_root

      obj_ids = obj.split(ancestry_delimiter).delete_if(&:blank?)
      primary_key_is_an_integer? ? obj_ids.map!(&:to_i) : obj_ids
    end

    def ancestry_depth_change(old_value, new_value)
      parse_ancestry_column(new_value).size - parse_ancestry_column(old_value).size
    end

    def concat(*args)
      if %w(sqlite sqlite3).include?(connection.adapter_name.downcase)
        args.join('||')
      else
        %{CONCAT(#{args.join(', ')})}
      end
    end

    def self.construct_depth_sql(table_name, ancestry_column, ancestry_delimiter)
      tmp = %{(LENGTH(#{table_name}.#{ancestry_column}) - LENGTH(REPLACE(#{table_name}.#{ancestry_column},'#{ancestry_delimiter}','')))}
      tmp += "/#{ancestry_delimiter.size}" if ancestry_delimiter.size > 1
      "(CASE WHEN #{table_name}.#{ancestry_column} IS NULL THEN 0 ELSE 1 + #{tmp} END)"
    end

    private

    def ancestry_validation_options(ancestry_primary_key_format)
      {
        format: {with: ancestry_format_regexp(ancestry_primary_key_format)},
        allow_nil: ancestry_nil_allowed?
      }
    end

    def ancestry_nil_allowed?
      true
    end

    def ancestry_format_regexp(primary_key_format)
      /\A#{primary_key_format}(#{Regexp.escape(ancestry_delimiter)}#{primary_key_format})*\z/.freeze
    end

    module InstanceMethods
      # optimization - better to go directly to column and avoid parsing
      def ancestors?
        read_attribute(self.class.ancestry_column) != self.class.ancestry_root
      end
      alias has_parent? ancestors?

      def ancestor_ids=(value)
        write_attribute(self.class.ancestry_column, self.class.generate_ancestry(value))
      end

      def ancestor_ids
        self.class.parse_ancestry_column(read_attribute(self.class.ancestry_column))
      end

      def ancestor_ids_in_database
        self.class.parse_ancestry_column(attribute_in_database(self.class.ancestry_column))
      end

      def ancestor_ids_before_last_save
        self.class.parse_ancestry_column(attribute_before_last_save(self.class.ancestry_column))
      end

      def parent_id_in_database
        self.class.parse_ancestry_column(attribute_in_database(self.class.ancestry_column)).last
      end

      def parent_id_before_last_save
        self.class.parse_ancestry_column(attribute_before_last_save(self.class.ancestry_column)).last
      end

      # optimization - better to go directly to column and avoid parsing
      def sibling_of?(node)
        read_attribute(self.class.ancestry_column) == node.read_attribute(node.class.ancestry_column)
      end

      # The ancestry value for this record's children
      # This can also be thought of as the ancestry value for the path
      # If this is a new record, it has no id, and it is not valid.
      # NOTE: This could have been called child_ancestry_in_database
      #       the child records were created from the version in the database
      def child_ancestry
        raise(Ancestry::AncestryException, I18n.t("ancestry.no_child_for_new_record")) if new_record?

        [attribute_in_database(self.class.ancestry_column), id].compact.join(self.class.ancestry_delimiter)
      end

      # The ancestry value for this record's old children
      # Currently used in an after_update via unscoped_descendants_before_last_save
      # to find the old children and bring them along (or to )
      # This is not valid in a new record's after_save.
      def child_ancestry_before_last_save
        if new_record? || (respond_to?(:previously_new_record?) && previously_new_record?)
          raise Ancestry::AncestryException, I18n.t("ancestry.no_child_for_new_record")
        end

        [attribute_before_last_save(self.class.ancestry_column), id].compact.join(self.class.ancestry_delimiter)
      end
    end
  end
end
