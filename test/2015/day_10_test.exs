defmodule AdventOfCode.Y2015.Day10Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1510

  alias AdventOfCode.Y2015.Day10, as: Solution

  test "Year 2015, Day 10, Part 1" do
    assert Solution.run_1() == 360_154
  end

  test "Year 2015, Day 10, Part 2" do
    assert Solution.run_2() == 5_103_798
  end
end
