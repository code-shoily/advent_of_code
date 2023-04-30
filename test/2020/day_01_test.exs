defmodule AdventOfCode.Y2020.Day01Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2001

  alias AdventOfCode.Y2020.Day01, as: Solution

  test "Year 2020, Day 1, Part 1" do
    assert Solution.run() == {1_014_624, 80_072_256}
  end
end
