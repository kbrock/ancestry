# frozen_string_literal: true

module Ancestry
  # store ancestry as /grandparent_id/parent_id/
  # root: a=/,id=1    children=#{a}#{id}/% == /1/%
  # 3:    a=/1/2/,id=3 children=#{a}#{id}/% == /1/2/3/%
  module MaterializedPath2
    include MaterializedPath

    def self.extended(base)
      base.send(:include, MaterializedPath::InstanceMethods)
      base.send(:include, InstanceMethods)
    end

    def ancestors_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        t = arel_table
        return where(t[primary_key].in(object.ancestor_ids))
      end

      ca_sql = child_ancestry_sql
      joins(
        "INNER JOIN (#{ancestor_scope(object).select(ancestry_column).to_sql}) ancestors_src" \
        " ON ancestors_src.#{ancestry_column} LIKE #{concat(ca_sql, "'%'")}"
      )
    end

    def indirects_of(object)
      sub = ancestor_scope(object).select("#{child_ancestry_sql} AS child_ancestry").to_sql
      joins(
        "INNER JOIN (#{sub}) indirects_src" \
        " ON #{table_name}.#{ancestry_column} LIKE #{concat("indirects_src.child_ancestry", "'%#{ancestry_delimiter}%'")}"
      )
    end

    def ordered_by_ancestry(order = nil)
      reorder(Arel::Nodes::Ascending.new(arel_table[ancestry_column]), order)
    end

    def descendants_by_ancestry(ancestry)
      arel_table[ancestry_column].matches("#{ancestry}%", nil, true)
    end

    def descendants_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        return where(descendant_conditions(object))
      end

      sub = ancestor_scope(object).select("#{child_ancestry_sql} AS child_ancestry").to_sql
      joins(
        "INNER JOIN (#{sub}) descendants_src" \
        " ON #{table_name}.#{ancestry_column} LIKE #{concat("descendants_src.child_ancestry", "'%'")}"
      )
    end

    def subtree_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        return descendants_of(object).or(where(primary_key => object.id))
      end

      sub = ancestor_scope(object).select("#{primary_key} AS #{primary_key}, #{child_ancestry_sql} AS child_ancestry").to_sql
      joins(
        "INNER JOIN (#{sub}) subtree_src" \
        " ON (#{table_name}.#{primary_key} = subtree_src.#{primary_key}" \
        " OR #{table_name}.#{ancestry_column} LIKE #{concat("subtree_src.child_ancestry", "'%'")})"
      )
    end

    def inpath_of(object)
      if object.is_a?(ancestry_base_class) && !object.is_a?(ActiveRecord::Relation)
        t = arel_table
        return where(t[primary_key].in(object.path_ids))
      end

      ca_sql = child_ancestry_sql
      sub = ancestor_scope(object).select("#{primary_key} AS #{primary_key}, #{ancestry_column}").to_sql
      joins(
        "INNER JOIN (#{sub}) inpath_src" \
        " ON (#{table_name}.#{primary_key} = inpath_src.#{primary_key}" \
        " OR inpath_src.#{ancestry_column} LIKE #{concat(ca_sql, "'%'")})"
      )
    end

    def descendant_conditions(object)
      descendants_by_ancestry(object.child_ancestry)
    end

    def ancestry_root
      ancestry_delimiter
    end

    def child_ancestry_sql(tbl = table_name)
      concat("#{tbl}.#{ancestry_column}", "#{tbl}.#{primary_key}", "'#{ancestry_delimiter}'")
    end

    def ancestry_depth_sql
      @ancestry_depth_sql ||= MaterializedPath2.construct_depth_sql(table_name, ancestry_column, ancestry_delimiter)
    end

    def generate_ancestry(ancestor_ids)
      if ancestor_ids.present? && ancestor_ids.any?
        "#{ancestry_delimiter}#{ancestor_ids.join(ancestry_delimiter)}#{ancestry_delimiter}"
      else
        ancestry_root
      end
    end

    # module method
    def self.construct_depth_sql(table_name, ancestry_column, ancestry_delimiter)
      tmp = %{(LENGTH(#{table_name}.#{ancestry_column}) - LENGTH(REPLACE(#{table_name}.#{ancestry_column},'#{ancestry_delimiter}','')))}
      tmp += "/#{ancestry_delimiter.size}" if ancestry_delimiter.size > 1
      "(#{tmp} -1)"
    end

    private

    def ancestry_nil_allowed?
      false
    end

    def ancestry_format_regexp(primary_key_format)
      /\A#{Regexp.escape(ancestry_delimiter)}(#{primary_key_format}#{Regexp.escape(ancestry_delimiter)})*\z/.freeze
    end

    module InstanceMethods
      # Please see notes for MaterializedPath#child_ancestry
      def child_ancestry
        raise(Ancestry::AncestryException, I18n.t("ancestry.no_child_for_new_record")) if new_record?

        "#{attribute_in_database(self.class.ancestry_column)}#{id}#{self.class.ancestry_delimiter}"
      end

      # Please see notes for MaterializedPath#child_ancestry_before_last_save
      def child_ancestry_before_last_save
        if new_record? || (respond_to?(:previously_new_record?) && previously_new_record?)
          raise(Ancestry::AncestryException, I18n.t("ancestry.no_child_for_new_record"))
        end

        "#{attribute_before_last_save(self.class.ancestry_column)}#{id}#{self.class.ancestry_delimiter}"
      end
    end
  end
end
