defmodule AdventOfCode.Y2020.Day09Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2009

  alias AdventOfCode.Y2020.Day09, as: Solution

  test "Year 2020, Day 9, Part 1" do
    assert Solution.run_1() == 15_353_384
  end

  test "Year 2020, Day 9, Part 2" do
    assert Solution.run_2() == 2_466_556
  end
end
