require_relative '../environment'

class UpdateTest < ActiveSupport::TestCase
  def test_node_creation_in_after_commit
    AncestryTestDatabase.with_model do |model|
      ch=[]
      model.instance_eval do
        attr_accessor :idx
        self.after_commit do
          # right now children.create! is not working in rails 6.1
          ch << children.scoping { model.create!(:idx => self.idx - 1) } if self.idx > 0
        end
      end
      model.create!(:idx => 3)
      # In the error case, the ancestry on each item will only contain the parent's id,
      # and not the entire ancestry tree.
      assert_equal '1/2/3', ch.first.ancestry
    end
  end
end
