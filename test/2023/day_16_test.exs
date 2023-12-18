defmodule AdventOfCode.Y2023.Day16Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2316

  alias AdventOfCode.Y2023.Day16, as: Solution

  test "Year 2023, Day 16 run/1" do
    assert Solution.run() == {6855, 7513}
  end
end
