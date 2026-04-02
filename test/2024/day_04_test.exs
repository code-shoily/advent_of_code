defmodule AdventOfCode.Y2024.Day04Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2404

  alias AdventOfCode.Y2024.Day04, as: Solution

  test "Year 2024, Day 4 run/1" do
    assert Solution.run() == {2575, 2041}
  end
end
