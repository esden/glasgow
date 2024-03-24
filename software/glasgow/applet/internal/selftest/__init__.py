import logging
import asyncio
from amaranth import *
from amaranth.build.res import ResourceError
from amaranth.build import Resource, Subsignal, Pins, Attrs

from ... import *

lvds_resources = [
    Resource("lvds_pins_n", 0,
        Pins("B2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33", PULLUP = 1)
    ),
    Resource("lvds_pins_n", 1,
        Pins("C3", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33", PULLUP = 1)
    ),
    Resource("lvds_pins_n", 2,
        Pins("C1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 3,
        Pins("D1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 4,
        Pins("D3", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 5,
        Pins("E3", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 6,
        Pins("F2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 7,
        Pins("F3", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 8,
        Pins("G2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 9,
        Pins("H3", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 10,
        Pins("H2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 11,
        Pins("K1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_n", 12,
        Pins("J2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 0,
        Pins("B1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33", PULLUP = 1)
    ),
    Resource("lvds_pins_p", 1,
        Pins("C4", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33", PULLUP = 1)
    ),
    Resource("lvds_pins_p", 2,
        Pins("C2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 3,
        Pins("E1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 4,
        Pins("D2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 5,
        Pins("E2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 6,
        Pins("F1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 7,
        Pins("F4", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 8,
        Pins("G1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 9,
        Pins("G3", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 10,
        Pins("H1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 11,
        Pins("J1", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
    Resource("lvds_pins_p", 12,
        Pins("K2", dir="io"),
        Attrs(IO_STANDARD="SB_LVCMOS33")
    ),
]

class SelfTestSubtarget(Elaboratable):
    def __init__(self, applet, target):
        self.reg_oe_a, applet.addr_oe_a = target.registers.add_rw(8)
        self.reg_o_a,  applet.addr_o_a  = target.registers.add_rw(8)
        self.reg_i_a,  applet.addr_i_a  = target.registers.add_ro(8)

        self.reg_oe_b, applet.addr_oe_b = target.registers.add_rw(8)
        self.reg_o_b,  applet.addr_o_b  = target.registers.add_rw(8)
        self.reg_i_b,  applet.addr_i_b  = target.registers.add_ro(8)

        self.reg_leds, applet.addr_leds = target.registers.add_rw(5)

        self.reg_oe_lvds_n, applet.addr_oe_lvds_n = target.registers.add_rw(13)
        self.reg_o_lvds_n,  applet.addr_o_lvds_n  = target.registers.add_rw(13)
        self.reg_i_lvds_n,  applet.addr_i_lvds_n  = target.registers.add_ro(13)
        self.reg_oe_lvds_p, applet.addr_oe_lvds_p = target.registers.add_rw(13)
        self.reg_o_lvds_p,  applet.addr_o_lvds_p  = target.registers.add_rw(13)
        self.reg_i_lvds_p,  applet.addr_i_lvds_p  = target.registers.add_ro(13)

        self.pins_a = [target.platform.request("port_a", n) for n in range(8)]
        self.pins_b = [target.platform.request("port_b", n) for n in range(8)]

        try:
            self.lvds_pins_n = [target.platform.request("lvds_pins_n", n) for n in range(13)]
            self.lvds_pins_p = [target.platform.request("lvds_pins_p", n) for n in range(13)]
        except ResourceError:
            self.lvds_pins_n = []
            self.lvds_pins_p = []

        try:
            self.leds = [target.platform.request("led", n) for n in range(5)]
        except ResourceError:
            self.leds = []

    def elaborate(self, platform):
        m = Module()

        m.d.comb += [pin.oe.o.eq(pin.io.oe) for pin in self.pins_a if hasattr(pin, "oe")]
        m.d.comb += [
            Cat(pin.io.oe for pin in self.pins_a).eq(self.reg_oe_a),
            Cat(pin.io.o for pin in self.pins_a).eq(self.reg_o_a),
            self.reg_i_a.eq(Cat(pin.io.i for pin in self.pins_a))
        ]

        m.d.comb += [pin.oe.o.eq(pin.io.oe) for pin in self.pins_b if hasattr(pin, "oe")]
        m.d.comb += [
            Cat(pin.io.oe for pin in self.pins_b).eq(self.reg_oe_b),
            Cat(pin.io.o for pin in self.pins_b).eq(self.reg_o_b),
            self.reg_i_b.eq(Cat(pin.io.i for pin in self.pins_b))
        ]

        m.d.comb += [
            Cat(pin.oe for pin in self.lvds_pins_n).eq(self.reg_oe_lvds_n),
            Cat(pin.o for pin in self.lvds_pins_n).eq(self.reg_o_lvds_n),
            self.reg_i_lvds_n.eq(Cat(pin.i for pin in self.lvds_pins_n)),
            Cat(pin.oe for pin in self.lvds_pins_p).eq(self.reg_oe_lvds_p),
            Cat(pin.o for pin in self.lvds_pins_p).eq(self.reg_o_lvds_p),
            self.reg_i_lvds_p.eq(Cat(pin.i for pin in self.lvds_pins_p))
        ]
        m.d.comb += Cat(pin.o for pin in self.leds).eq(self.reg_leds)

        return m


class SelfTestApplet(GlasgowApplet):
    logger = logging.getLogger(__name__)
    help = "diagnose hardware faults"
    description = """
    Diagnose hardware faults.

    Currently, shorts and opens on I/O lines can be detected.

    Test modes:
        * leds: test indicator LED functionality
          (Vio will be enabled)
        * pins-int: detect shorts on traces between FPGA and I/O buffers
          (no requirements)
        * pins-ext: detect shorts and opens on traces between FPGA and I/O connector
          (all pins on all I/O connectors must be floating)
        * pins-pull: detects faults in pull resistor circuits
          (all pins on all I/O connectors must be floating)
        * pins-loop: detect faults anywhere in the I/O circuits
          (pins A0:A7 must be connected to B0:B7)
        * voltage: detect ADC, DAC or LDO faults
          (on all ports, Vsense and Vio pins must be connected)
        * loopback: detect faults in USB FIFO traces
          (no requirements)
        * lvds: test the LVDS connector
          (the test plug needs to be inserted)
    """

    __all_modes = ["leds", "pins-int", "pins-ext", "pins-pull", "pins-loop", "voltage", "loopback", "lvds"]
    __default_modes = ["pins-int", "loopback"]

    def build(self, target, args):
        if device.revision >= C0:
            target.platform.add_resources(lvds_resources)

        target.add_submodule(SelfTestSubtarget(applet=self, target=target))

        self.mux_interface_1 = iface_1 = target.multiplexer.claim_interface(self, None)
        self.mux_interface_2 = iface_2 = target.multiplexer.claim_interface(self, None)

        in_fifo_1, out_fifo_1 = iface_1.get_in_fifo(), iface_1.get_out_fifo()
        in_fifo_2, out_fifo_2 = iface_2.get_in_fifo(), iface_2.get_out_fifo()
        m = Module()
        m.d.comb += [
            in_fifo_1.w_data.eq(out_fifo_1.r_data),
            in_fifo_1.w_en.eq(out_fifo_1.r_rdy),
            out_fifo_1.r_en.eq(in_fifo_1.w_rdy),
            in_fifo_2.w_data.eq(out_fifo_2.r_data),
            in_fifo_2.w_en.eq(out_fifo_2.r_rdy),
            out_fifo_2.r_en.eq(in_fifo_2.w_rdy),
        ]
        target.add_submodule(m)

    @classmethod
    def add_run_arguments(cls, parser, access):
        parser.add_argument(
            dest="modes", metavar="MODE", type=str, nargs="*", choices=[[]] + cls.__all_modes,
            help="run self-test mode MODE (default: {})".format(" ".join(cls.__default_modes)))

    async def run(self, device, args):
        return None

    async def interact(self, device, args, iface):

        async def set_lvds_oe(bits):
            await device.write_register(self.addr_oe_lvds_p, (bits >> 0) & 0x1fff, width=2)
            await device.write_register(self.addr_oe_lvds_n, (bits >> 13) & 0x1fff, width=2)

        async def set_lvds_o(bits):
            await device.write_register(self.addr_o_lvds_p, (bits >> 0) & 0x1fff, width=2)
            await device.write_register(self.addr_o_lvds_n, (bits >> 13) & 0x1fff, width=2)

        async def get_lvds_i():
            return ((await device.read_register(self.addr_i_lvds_p, width=2) << 0) |
                    (await device.read_register(self.addr_i_lvds_n, width=2) << 13))

        async def set_oe(bits):
            await device.write_register(self.addr_oe_a, (bits >> 0) & 0xff)
            await device.write_register(self.addr_oe_b, (bits >> 8) & 0xff)

        async def set_o(bits):
            await device.write_register(self.addr_o_a,  (bits >> 0) & 0xff)
            await device.write_register(self.addr_o_b,  (bits >> 8) & 0xff)

        async def set_pull(bits_o, bits_oe):
            pull_low  = {x for x in range(16) if bits_oe & (1 << x) and not bits_o & (1 << x)}
            pull_high = {x for x in range(16) if bits_oe & (1 << x) and     bits_o & (1 << x)}
            await device.set_pulls("AB", pull_low, pull_high)

        async def get_i():
            return ((await device.read_register(self.addr_i_a) << 0) |
                    (await device.read_register(self.addr_i_b) << 8))

        async def reset_pins(bits):
            await set_o(bits)
            await set_oe(0xffff)
            await asyncio.sleep(0.001)
            await set_oe(0x0000)

        async def check_pins(oe, o, use_pull):
            if use_pull:
                await set_pull(o, oe)
            else:
                await set_o(o)
                await set_oe(oe)
            i = await get_i()
            desc = f"oe={oe:016b} o={o:016b} i={i:016b}"
            return i, desc

        pin_names = sum([["%s%d" % (p, n) for n in range(8)] for p in ("A", "B")], [])
        def decode_pins(bits):
            result = set()
            for bit in range(0, 16):
                if bits & (1 << bit):
                    result.add(pin_names[bit])
            return result

        passed = True
        report = []
        for mode in args.modes or self.__default_modes:
            self.logger.info("running self-test mode %s", mode)

            if mode == "lvds":
                group_masks = {
                    # 111           111
                    # 2109876543210 2109876543210
                    # nnnnnnnnnnnnn ppppppppppppp
                    # CBACACACCAACB DABDBDBDDBBDA
                    "A": 0b0010101001100_0100000000001, # Group A Mask
                    "B": 0b0100000000001_0010101001100, # Group B Mask
                    "C": 0b1001010110010_0000000000000, # Group C Mask
                    "D": 0b0000000000000_1001010110010, # Group D Mask
                }
                # Set Z0_P&N and Z1_P&N as input rest as output
                await set_lvds_o(0b0000000000000_0000000000000)
                await set_lvds_oe(0b0000000000000_0000000000000)
                await asyncio.sleep(0.01)
                i = await get_lvds_i()
                if i != 0b1111111111111_1111111111111:
                    passed = False
                    report.append((mode, f"fail init got {i:026b} expected ".format(i)))
                await set_lvds_oe(0b0000000000000_0000000000100)
                await asyncio.sleep(0.01)
                i = await get_lvds_i()
                for bit in range(22):
                    if bit < 11:
                        oe_mask = 0b0000000000000_0000000000100 << bit
                    else:
                        oe_mask = 0b0000000000100_0000000000000 << bit - 11
                    await set_lvds_oe(oe_mask)
                    await asyncio.sleep(0.01)
                    i = await get_lvds_i()
                    expect_i = 0b1111111111111_1111111111111
                    expect_group = None
                    for group, mask in group_masks.items():
                        if mask & oe_mask != 0:
                            expect_group = group
                            expect_i = expect_i & ~mask
                    if i != expect_i:
                        passed = False
                        report.append((mode, f"failed group {expect_group} expected {expect_i:026b} got {i:026b}"))
                    #print(f"oe: {oe_mask:026b} i: {i:026b} ei {expect_i:026b} group {expect_group}")

            if mode == "leds":
                self.logger.warning("power cycle the device to restore LED function")

                led_state = 0b11111111111_00000000000
                while True:
                    await device.test_leds(
                        (led_state & 0b1111) >> 0)
                    await device.write_register(self.addr_leds,
                        (led_state & 0b11111_0000) >> 4)
                    await device.set_voltage("A",
                        3.3 if (led_state & 0b1_000000000) >> 9 else 0.0)
                    await device.set_voltage("B",
                        3.3 if (led_state & 0b1_0000000000) >> 10 else 0.0)
                    await asyncio.sleep(0.1)
                    led_state = ((led_state << 1) | (led_state >> 21)) & ~(1 << 22)

            if mode in ("pins-int", "pins-ext", "pins-pull"):
                if device.revision >= "C0":
                    raise GlasgowAppletError(f"mode {mode} is broken on device revision "
                                             f"{device.revision}")

                if mode == "pins-int":
                    await device.set_voltage("AB", 0)

                    # disable the IO-buffers (FXMA108) on revAB to not influence the external ports
                    # no effect on other revisions
                    await device._iobuf_enable(False)
                elif mode in ("pins-ext", "pins-pull"):
                    await device.set_voltage("AB", 3.3)

                    # re-enable the IO-buffers (FXMA108) on revAB
                    # no effect on other revisions
                    await device._iobuf_enable(True)
                use_pull = (mode == "pins-pull")

                for bits in (0x0000, 0xffff):
                    await reset_pins(bits)
                    i, desc = await check_pins(bits, bits, use_pull=use_pull)
                    self.logger.debug("%s: %s", mode, desc)
                    if bits == 0x0000:
                        fail_high = decode_pins(i)
                    if bits == 0xffff:
                        fail_low  = decode_pins(~i)

                shorted = []
                for bit in range(0, 16):
                    await reset_pins(bits=0x0000)
                    i, desc = await check_pins(1 << bit, 1 << bit, use_pull=use_pull)
                    self.logger.debug("%s: %s", mode, desc)

                    if i != 1 << bit:
                        pins = decode_pins(i) - fail_high
                        if len(pins) > 1 and pins not in shorted:
                            shorted.append(pins)
                        passed = False

                if fail_high:
                    report.append((mode, "fail high: {}".format(" ".join(sorted(fail_high)))))
                if fail_low:
                    report.append((mode, "fail low: {}".format(" ".join(sorted(fail_low)))))
                for pins in shorted:
                    report.append((mode, "fail short: {}".format(" ".join(sorted(pins)))))

                await device.set_voltage("AB", 0)

                # re-enable the IO-buffers (FXMA108) on revAB, they are on by default
                # no effect on other revisions
                await device._iobuf_enable(True)

            if mode == "pins-loop":
                await device.set_voltage("AB", 3.3)

                broken = []
                for bit in range(0, 8):
                    for o in (1 << bit, 1 << (15 - bit)):
                        await reset_pins(bits=0x0000)
                        i, desc = await check_pins(o, o, use_pull=False)
                        self.logger.debug("%s: %s", mode, desc)

                        e = ((o << 8) | o) if (o & 0xFF) else (o | (o >> 8))
                        if i != e:
                            passed = False
                            pins = decode_pins(i | e)
                            report.append((mode, "fault: {}".format(" ".join(pins))))
                            break

                await device.set_voltage("AB", 0)

            if mode == "voltage":
                await device.set_voltage("AB", 0)

                for port in ("A", "B"):
                    for vout in (1.8, 2.7, 3.3, 5.0):
                        await device.set_voltage(port, vout)
                        await asyncio.sleep(0.1)
                        vin = await device.measure_voltage(port)
                        self.logger.debug("port {}: Vio={:.1f} Vsense={:.2f}"
                                          .format(port, vout, vin))

                        if abs(vout - vin) / vout > 0.05:
                            passed = False
                            report.append((mode, "port {} out of ±5% tolerance: "
                                                 "Vio={:.2f} Vsense={:.2f}"
                                                 .format(port, vout, vin)))

                    await device.set_voltage(port, 0)

            if mode == "loopback":
                iface_1 = await device.demultiplexer.claim_interface(
                    self, self.mux_interface_1, None)
                iface_2 = await device.demultiplexer.claim_interface(
                    self, self.mux_interface_2, None)

                data_1 = b"The quick brown fox jumps over the lazy dog.\x55\xaa"
                data_2 = bytes(reversed(data_1))

                for iface, data, ep_out, ep_in in (
                    (iface_1, data_1, "EP2OUT", "EP6IN"),
                    (iface_2, data_2, "EP4OUT", "EP8IN"),
                ):
                    try:
                        await iface.write(data)
                        await asyncio.wait_for(iface.flush(), timeout=0.1)
                    except asyncio.TimeoutError:
                        passed = False
                        report.append((mode, f"USB {ep_out} timeout"))
                        continue

                    try:
                        received = await asyncio.wait_for(iface.read(len(data)), timeout=0.1)
                    except asyncio.TimeoutError:
                        passed = False
                        report.append((mode, f"USB {ep_in} timeout"))
                        continue

                    if received != data:
                        passed = False
                        report.append((mode, "USB {}->{} read-write mismatch"
                                             .format(ep_out, ep_in)))

        if passed:
            self.logger.info("self-test: PASS")
        else:
            for (mode, message) in report:
                self.logger.error("%s: %s", mode, message)
            raise GlasgowAppletError("self-test: FAIL")

    @classmethod
    def tests(cls):
        from . import test
        return test.SelfTestAppletTestCase
