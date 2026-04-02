defmodule AdventOfCode.Y2025.Day05Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2505

  alias AdventOfCode.Y2025.Day05, as: Solution

  test "Year 2025, Day 5 run/1" do
    assert Solution.run() == {505, 344_423_158_480_189}
  end
end
