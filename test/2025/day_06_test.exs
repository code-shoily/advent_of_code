defmodule AdventOfCode.Y2025.Day06Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2506

  alias AdventOfCode.Y2025.Day06, as: Solution

  test "Year 2025, Day 6 run/1" do
    assert Solution.run() == {4_805_473_544_166, 8_907_730_960_817}
  end
end
