defmodule AdventOfCode.Y2025.Day02Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2502

  alias AdventOfCode.Y2025.Day02, as: Solution

  test "Year 2025, Day 2 run/1" do
    assert Solution.run() == {44_854_383_294, 55_647_141_923}
  end
end
