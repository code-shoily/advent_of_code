defmodule AdventOfCode.Y2025.Day07Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2507

  alias AdventOfCode.Y2025.Day07, as: Solution

  test "Year 2025, Day 7 run/1" do
    assert Solution.run() == {1518, 25_489_586_715_621}
  end
end
