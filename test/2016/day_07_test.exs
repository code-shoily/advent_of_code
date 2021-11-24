defmodule AdventOfCode.Y2016.Day07Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1607

  alias AdventOfCode.Y2016.Day07, as: Solution

  test "Year 2016, Day 7, Part 1" do
    assert Solution.run_1() == 105
  end

  test "Year 2016, Day 7, Part 2" do
    assert Solution.run_2() == 258
  end
end
