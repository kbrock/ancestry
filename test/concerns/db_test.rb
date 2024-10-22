require_relative '../environment'

class DbTest < ActiveSupport::TestCase
  def test_does_not_load_database
    c = Class.new(ActiveRecord::Base) do
      self.table_name = "table"

      def self.connection
        raise "Oh No - tried to connect to database"
      end
    end

    c.send(:has_ancestry)
  end
end
