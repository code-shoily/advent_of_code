defmodule AdventOfCode.Y2023.Day11Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2311

  alias AdventOfCode.Y2023.Day11, as: Solution

  test "Year 2023, Day 11 run/1" do
    assert Solution.run() == {9_795_148, 650_672_493_820}
  end
end
