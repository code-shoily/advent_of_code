defmodule AdventOfCode.Y2015.Day10Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1510

  alias AdventOfCode.Y2015.Day10, as: Solution

  test "Year 2015, Day 10" do
    assert Solution.run() == {360_154, 5_103_798}
  end
end
