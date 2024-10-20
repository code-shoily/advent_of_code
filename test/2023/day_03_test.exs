defmodule AdventOfCode.Y2023.Day03Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2303

  alias AdventOfCode.Y2023.Day03, as: Solution

  test "Year 2023, Day 3 run/1" do
    assert Solution.run() == {539_713, 84_159_075}
  end
end
