defmodule AdventOfCode.Y2015.Day09Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1509

  alias AdventOfCode.Y2015.Day09, as: Solution

  test "Year 2015, Day 9, Part 1" do
    assert Solution.run_1() == 117
  end

  test "Year 2015, Day 9, Part 2" do
    assert Solution.run_2() == 909
  end
end
