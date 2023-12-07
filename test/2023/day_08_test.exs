defmodule AdventOfCode.Y2023.Day08Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2308

  alias AdventOfCode.Y2023.Day08, as: Solution

  test "Year 2023, Day 8 run/1" do
    assert Solution.run() == {20_093, 22_103_062_509_257}
  end
end
