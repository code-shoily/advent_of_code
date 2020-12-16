defmodule AdventOfCode.Y2020.Day15ETSTest do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2015_ets

  alias AdventOfCode.Y2020.Day15ETS, as: Solution

  test "Year 2020, Day 15, Part 1: Process" do
    assert Solution.run_1() == 468
  end

  test "Year 2020, Day 15, Part 2: Process" do
    assert Solution.run_2() == 1_801_753
  end
end
