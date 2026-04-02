defmodule AdventOfCode.Y2025.Day09Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2509

  alias AdventOfCode.Y2025.Day09, as: Solution

  test "Year 2025, Day 9 run/1" do
    assert Solution.run() == {4_746_238_001, 1_552_139_370}
  end
end
