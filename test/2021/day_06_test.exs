defmodule AdventOfCode.Y2021.Day06Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2106

  alias AdventOfCode.Y2021.Day06, as: Solution

  test "Year 2021, Day 6" do
    assert Solution.run() == {350_149, 1_590_327_954_513}
  end
end
