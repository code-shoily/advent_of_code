defmodule AdventOfCode.Y2017.Day10Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1710

  alias AdventOfCode.Y2017.Day10, as: Solution

  test "Year 2017, Day 10 run/1" do
    assert Solution.run() == {9656, "20b7b54c92bf73cf3e5631458a715149"}
  end
end
