defmodule AdventOfCode.Y2016.Day6Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y166

  alias AdventOfCode.Y2016.Day6, as: Solution

  test "Year 2016, Day 6, Part 1" do
    assert Solution.run_1() == "qzedlxso"
  end

  test "Year 2016, Day 6, Part 2" do
    assert Solution.run_2() == "ucmifjae"
  end
end
