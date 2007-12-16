class Mine
  attr_accessor :var
  def inspect
    throw "Foo"
  end
  def initialize
    @var = 'initialized'
  end
end
class MineAlso
  attr_accessor :var
  def inspect
    throw "Foo"
  end
  def to_s
    throw "bar"
  end
  def initialize
    @var = 'initialized'
  end
end
def testMine
  x = Mine.new
  y = 5
end
def testMineAlso
  x = MineAlso.new
  y = 5
end
testMine
testMineAlso
y = 2
