defmodule AdventOfCode.Y2017.Day16Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1716

  alias AdventOfCode.Y2017.Day16, as: Solution

  test "Year 2017, Day 16 run/1" do
    assert Solution.run() == {"ehdpincaogkblmfj", "bpcekomfgjdlinha"}
  end
end
