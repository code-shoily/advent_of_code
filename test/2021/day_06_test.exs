defmodule AdventOfCode.Y2021.Day06Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2106

  alias AdventOfCode.Y2021.Day06, as: Solution

  test "Year 2021, Day 6, Part 1" do
    assert Solution.run_1() == 350_149
  end

  test "Year 2021, Day 6, Part 2" do
    assert Solution.run_2() == 1_590_327_954_513
  end
end
