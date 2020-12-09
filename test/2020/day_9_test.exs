defmodule AdventOfCode.Y2020.Day9Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y209

  alias AdventOfCode.Y2020.Day9, as: Solution

  test "Year 2020, Day 9, Part 1" do
    assert Solution.run_1() == 15_353_384
  end

  test "Year 2020, Day 9, Part 2" do
    assert Solution.run_2() == 2_466_556
  end
end
