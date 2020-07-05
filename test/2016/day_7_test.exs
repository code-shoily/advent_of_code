defmodule AdventOfCode.Y2016.Day7Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y167

  alias AdventOfCode.Y2016.Day7, as: Solution

  @tag :p1
  test "Year 2016, Day 7, Part 1" do
    assert Solution.run_1() == 105
  end

  @tag :p2
  test "Year 2016, Day 7, Part 2" do
    assert Solution.run_2() == 258
  end
end
