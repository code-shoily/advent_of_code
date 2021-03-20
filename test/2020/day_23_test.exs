defmodule AdventOfCode.Y2020.Day23Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2023

  alias AdventOfCode.Y2020.Day23, as: Solution

  test "Year 2020, Day 23, Part 1" do
    assert Solution.run_1() == "43786952"
  end

  test "Year 2020, Day 23, Part 2" do
    assert Solution.run_2() == nil
  end
end
