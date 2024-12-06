defmodule AdventOfCode.Y2024.Day01Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2401

  alias AdventOfCode.Y2024.Day01, as: Solution

  test "Year 2024, Day 1 run/1" do
    assert Solution.run() == {2_742_123, 21_328_497}
  end
end
