defmodule AdventOfCode.Y2025.Day01Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2501

  alias AdventOfCode.Y2025.Day01, as: Solution

  test "Year 2025, Day 1 run/1" do
    assert Solution.run() == {1059, 6305}
  end
end
