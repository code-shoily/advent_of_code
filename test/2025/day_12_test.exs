defmodule AdventOfCode.Y2025.Day12Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2512

  alias AdventOfCode.Y2025.Day12, as: Solution

  test "Year 2025, Day 12 run/1" do
    assert Solution.run() == {510, :done}
  end
end
