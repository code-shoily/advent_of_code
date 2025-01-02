defmodule AdventOfCode.Y2024.Day12Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2412

  alias AdventOfCode.Y2024.Day12, as: Solution

  test "Year 2024, Day 12 run/1" do
    assert Solution.run() == {1_304_764, nil}
  end
end
