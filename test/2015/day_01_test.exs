defmodule AdventOfCode.Y2015.Day1Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1501

  alias AdventOfCode.Y2015.Day01, as: Solution

  test "Year 2015, Day 1, Part 1" do
    assert Solution.run_1() == 232
  end

  test "Year 2015, Day 1, Part 2" do
    assert Solution.run_2() == 1783
  end
end
