defmodule AdventOfCode.Y2023.Day01Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2201

  alias AdventOfCode.Y2023.Day01, as: Solution

  test "Year 2023, Day 1 run/1" do
    assert Solution.run() == {53_194, 54_249}
  end
end
