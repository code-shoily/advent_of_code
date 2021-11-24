defmodule AdventOfCode.Y2018.Day08Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1808

  alias AdventOfCode.Y2018.Day08, as: Solution

  test "Year 2018, Day 8, Part 1" do
    assert Solution.run_1() == 40_701
  end

  @tag :skip
  test "Year 2018, Day 8, Part 2" do
    assert Solution.run_2() == nil
  end
end
