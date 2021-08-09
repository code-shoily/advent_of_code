defmodule AdventOfCode.Y2016.Day06Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1606

  alias AdventOfCode.Y2016.Day06, as: Solution

  test "Year 2016, Day 6, Part 1" do
    assert Solution.run_1() == "qzedlxso"
  end

  test "Year 2016, Day 6, Part 2" do
    assert Solution.run_2() == "ucmifjae"
  end
end
