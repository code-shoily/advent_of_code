defmodule AdventOfCode.Y2025.Day10Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2510

  alias AdventOfCode.Y2025.Day10, as: Solution

  test "Year 2025, Day 10 run/1" do
    assert Solution.run() == {375, 15_377}
  end
end
