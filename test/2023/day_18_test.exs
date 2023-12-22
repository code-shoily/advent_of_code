defmodule AdventOfCode.Y2023.Day18Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2318

  alias AdventOfCode.Y2023.Day18, as: Solution

  test "Year 2023, Day 18 run/1" do
    assert Solution.run() == {72_821, 127_844_509_405_501}
  end
end
