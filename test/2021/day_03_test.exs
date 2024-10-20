defmodule AdventOfCode.Y2021.Day03Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2103

  alias AdventOfCode.Y2021.Day03, as: Solution

  test "Year 2021, Day 3" do
    assert Solution.run() == {1_540_244, 4_203_981}
  end
end
