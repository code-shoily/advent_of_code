defmodule AdventOfCode.Y2017.Day18.Duet do
  alias __MODULE__

  defstruct [:instructions, seq: 0, registers: %{}, last: nil]

  def play_sound(%Duet{seq: seq, last: last} = duet, frequency) do
    case frequency do
      {:val, frequency} ->
        %Duet{duet | seq: seq + 1, last: (frequency == 0 && last) || frequency}

      {:ref, reg} ->
        %Duet{duet | seq: seq + 1, last: max(reg_val(duet, reg), 0)}
    end
  end

  def set_value(%Duet{seq: seq, registers: registers} = duet, to, from) do
    val =
      case from do
        {:val, val} -> val
        {:ref, reg} -> reg_val(duet, reg)
      end

    %Duet{duet | seq: seq + 1, registers: Map.put(registers, to, val)}
  end

  def recover_last(%Duet{seq: seq, last: last} = duet, {:ref, reg}),
    do: (reg_val(duet, reg) == 0 && %Duet{duet | seq: seq + 1}) || last

  def binary_op(%Duet{registers: registers, seq: seq} = duet, op, register, source) do
    val =
      case source do
        {:ref, reg} -> reg_val(duet, reg)
        {:val, val} -> val
      end

    %Duet{
      duet
      | seq: seq + 1,
        registers: Map.put(registers, register, apply(Kernel, op, [reg_val(duet, register), val]))
    }
  end

  def jump_if_nonzero(%Duet{seq: seq} = duet, register, step_source) do
    ctrl_reg = reg_val(duet, register)

    jump_val =
      case step_source do
        {:val, val} -> (ctrl_reg > 0 && val) || 1
        {:ref, reg} -> (ctrl_reg > 0 && reg_val(duet, reg)) || 1
      end

    %Duet{duet | seq: seq + jump_val}
  end

  def reg_val(%Duet{registers: regs}, reg), do: Map.get(regs, reg, 0)
end
