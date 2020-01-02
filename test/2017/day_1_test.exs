defmodule AdventOfCode.Y2017.Day1Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y171

  alias AdventOfCode.Y2017.Day1, as: Solution

  test "Year 2017, Day 1, Part 1" do
    assert Solution.run_1() == 1089
  end

  test "Year 2017, Day 1, Part 2" do
    assert Solution.run_2() == 1156
  end
end
