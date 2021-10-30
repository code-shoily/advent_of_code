defmodule AdventOfCode.Y2015.Day11Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1511

  alias AdventOfCode.Y2015.Day11, as: Solution

  test "Year 2015, Day 11, Part 1" do
    assert Solution.run_1() == "cqjxxyzz"
  end

  test "Year 2015, Day 11, Part 2" do
    assert Solution.run_2() == "cqkaabcc"
  end
end
