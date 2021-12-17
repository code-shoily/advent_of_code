defmodule AdventOfCode.Y2021.Day15Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2115

  alias AdventOfCode.Y2021.Day15, as: Solution

  test "Year 2021, Day 15, Part 1" do
    assert Solution.run_1() == 583
  end

  test "Year 2021, Day 15, Part 2" do
    assert Solution.run_2() == 2927
  end
end
