defmodule AdventOfCode.Y2025.Day11Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2511

  alias AdventOfCode.Y2025.Day11, as: Solution

  test "Year 2025, Day 11 run/1" do
    assert Solution.run() == {428, 331_468_292_364_745}
  end
end
