defmodule AdventOfCode.Y2020.Day02Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2002

  alias AdventOfCode.Y2020.Day02, as: Solution

  test "Year 2020, Day 2, Part 1" do
    assert Solution.run_1() == 607
  end

  test "Year 2020, Day 2, Part 2" do
    assert Solution.run_2() == 321
  end
end
