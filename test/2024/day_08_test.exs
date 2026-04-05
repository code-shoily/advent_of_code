defmodule AdventOfCode.Y2024.Day08Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2408

  alias AdventOfCode.Y2024.Day08, as: Solution

  test "Year 2024, Day 8 run/0" do
    assert Solution.run() == {291, 1015}
  end
end
