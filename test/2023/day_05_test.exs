defmodule AdventOfCode.Y2023.Day05Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2305

  alias AdventOfCode.Y2023.Day05, as: Solution

  test "Year 2023, Day 5 run/1" do
    assert Solution.run() == {3_374_647, 6_082_852}
  end
end
