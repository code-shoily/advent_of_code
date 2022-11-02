defmodule AdventOfCode.Y2015.Day14Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1514

  alias AdventOfCode.Y2015.Day14, as: Solution

  test "Year 2015, Day 14, Part 1" do
    assert Solution.run_1() == 2640
  end

  test "Year 2015, Day 14, Part 2" do
    assert Solution.run_2() == 1102
  end
end
