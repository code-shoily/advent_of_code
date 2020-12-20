defmodule AdventOfCode.Y2020.Day20Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2020

  alias AdventOfCode.Y2020.Day20, as: Solution

  test "Year 2020, Day 20, Part 1" do
    assert Solution.run_1() == 29_125_888_761_511
  end

  test "Year 2020, Day 20, Part 2" do
    assert Solution.run_2() == nil
  end
end
