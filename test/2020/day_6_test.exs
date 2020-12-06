defmodule AdventOfCode.Y2020.Day6Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y206

  alias AdventOfCode.Y2020.Day6, as: Solution

  test "Year 2020, Day 6, Part 1" do
    assert Solution.run_1() == 6885
  end

  test "Year 2020, Day 6, Part 2" do
    assert Solution.run_2() == 3550
  end
end
