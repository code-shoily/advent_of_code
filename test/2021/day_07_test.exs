defmodule AdventOfCode.Y2021.Day07Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2107

  alias AdventOfCode.Y2021.Day07, as: Solution

  test "Year 2021, Day 7, Part 1" do
    assert Solution.run_1() == 344_138
  end

  test "Year 2021, Day 7, Part 2" do
    assert Solution.run_2() == 94_862_124
  end
end
