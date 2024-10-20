defmodule AdventOfCode.Y2022.Day01Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2201

  alias AdventOfCode.Y2022.Day01, as: Solution

  test "Year 2022, Day 1 run/1" do
    assert Solution.run() == {70_720, 207_148}
  end
end
