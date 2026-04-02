defmodule AdventOfCode.Y2024.Day06Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2406

  alias AdventOfCode.Y2024.Day06, as: Solution

  test "Year 2024, Day 6 run/1" do
    assert Solution.run() == {4982, 1663}
  end
end
