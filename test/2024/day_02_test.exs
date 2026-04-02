defmodule AdventOfCode.Y2024.Day02Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2402

  alias AdventOfCode.Y2024.Day02, as: Solution

  test "Year 2024, Day 2 run/1" do
    assert Solution.run() == {486, 540}
  end
end
