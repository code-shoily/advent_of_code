defmodule AdventOfCode.Y2017.Day5Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1705

  alias AdventOfCode.Y2017.Day05, as: Solution

  test "Year 2017, Day 5, Part 1" do
    assert Solution.run_1() == 372_671
  end

  test "Year 2017, Day 5, Part 2" do
    assert Solution.run_2() == 25_608_480
  end
end
