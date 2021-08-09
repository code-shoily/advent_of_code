defmodule AdventOfCode.Y2020.Day06Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2006

  alias AdventOfCode.Y2020.Day06, as: Solution

  test "Year 2020, Day 6, Part 1" do
    assert Solution.run_1() == 6885
  end

  test "Year 2020, Day 6, Part 2" do
    assert Solution.run_2() == 3550
  end
end
