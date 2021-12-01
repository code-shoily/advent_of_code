defmodule AdventOfCode.Y2021.Day01Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2101

  alias AdventOfCode.Y2021.Day01, as: Solution

  test "Year 2021, Day 1, Part 1" do
    assert Solution.run_1() == 1139
  end

  test "Year 2021, Day 1, Part 2" do
    assert Solution.run_2() == 1103
  end
end
