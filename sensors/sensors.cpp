#include "mbed.h"
#include "DS1820.h"
#include "nRF24L01P.h"

#define TEMPERATURE_PIN D2
#define LIQUID_LVL_PIN  A0
#define GREYSCALE_PIN   A2

#define DATA_RATE       NRF24L01P_DATARATE_1_MBPS
#define POWER           NRF24L01P_TX_PWR_ZERO_DB
#define TX_ADDRESS      ((unsigned long long) 0xABCDEF01)
#define RX_ADDRESS      ((unsigned long long) 0xABCDEF10)
#define CHANNEL         11
#define TRANSFER_SIZE   32
#define MAX_PROBES      16
#define MAX_VOLTAGE     2.7f
#define RETRANSMITIONS  3
#define LOOP_TIME       2
#define ACK_TIME        0.15

Serial pc(USBTX, USBRX); // tx, rx
nRF24L01P radio(PB_15, PB_14, PB_13, PB_12, PB_1, PB_2);    // mosi, miso, sck, csn, ce, irq
DS1820* probe[MAX_PROBES];
AnalogIn liquidLevel(LIQUID_LVL_PIN);
AnalogIn grayscale(GREYSCALE_PIN);

// Compute liquid level from liquid voltage
const int SIZE = 11;
const float vx[] = {0.0, 1.3, 1.53, 1.62, 1.69, 1.74, 1.77, 1.81, 1.84, 1.86, 1.88};
const float vy[] = {0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 4.8};
// https://www.waveshare.com/w/upload/7/78/Liquid-Level-Sensor-UserManual.pdf
float convert_voltage_to_liquid_level(float voltage) {
    // Sensor falsifies results if imersed for more than half
    for (int i = 1; i < SIZE; ++i)
        if (voltage < vx[i])
            return (voltage * (vy[i] - vy[i - 1]) + vy[i - 1] * vx[i] - vx[i - 1] * vy[i]) / (vx[i] - vx[i - 1]);
    return 4.9;
}

char buf[TRANSFER_SIZE], ack_buf[TRANSFER_SIZE];

void measure() {
    float liquid_voltage = liquidLevel.read() * MAX_VOLTAGE,
          liquid_level = convert_voltage_to_liquid_level(liquid_voltage);
    probe[0]->convertTemperature(true, DS1820::all_devices);
    snprintf(buf, TRANSFER_SIZE, "L %.3f G %.3f T %.3lf ",
             liquid_level, grayscale.read(), probe[0]->temperature());
}

void sendbuf() {
    for (int i = 0; i < RETRANSMITIONS; ++i) {
        pc.printf("SEND %s\r\n", buf);
        radio.write(NRF24L01P_PIPE_P0, buf, TRANSFER_SIZE);
        wait(ACK_TIME);
        if (radio.readable(NRF24L01P_PIPE_P1)) {
            radio.read(NRF24L01P_PIPE_P1, ack_buf, TRANSFER_SIZE);
            pc.printf("OK\r\n");
            return;
        }
    }
}

int main() {
    // Initialize radio
    pc.baud(115200);
    radio.setAirDataRate(DATA_RATE);
    radio.setRfOutputPower(POWER);
    radio.setRfFrequency(NRF24L01P_MIN_RF_FREQUENCY + 4 * CHANNEL);
    radio.setCrcWidth(NRF24L01P_CRC_8_BIT);
    radio.setTxAddress(TX_ADDRESS, 4);
    radio.setRxAddress(TX_ADDRESS, 4, NRF24L01P_PIPE_P0);
    radio.setRxAddress(RX_ADDRESS, 4, NRF24L01P_PIPE_P1);
    radio.setTransferSize(TRANSFER_SIZE, NRF24L01P_PIPE_P0);
    radio.setTransferSize(TRANSFER_SIZE, NRF24L01P_PIPE_P1);
    radio.enable();
    radio.setReceiveMode();

    // Initialize the probe array to DS1820 objects
    int num_devices = 0;
    while (DS1820::unassignedProbe(TEMPERATURE_PIN)) {
        probe[num_devices] = new DS1820(TEMPERATURE_PIN);
        num_devices++;
        if (num_devices == MAX_PROBES)
            break;
    }

    // Main loop
    Timer timer_loop;
    timer_loop.start();
    while (1) {
        timer_loop.reset();
        measure();
        sendbuf();
        pc.printf("Loop time: %f\r\n", timer_loop.read());
        wait(LOOP_TIME - timer_loop.read());
    }
}
