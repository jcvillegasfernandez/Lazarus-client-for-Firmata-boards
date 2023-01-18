Una serie de módulos en forma de paquete de componentes para Lazarus (free Pascal) en dispositivos con Firmata, ConfigurableFirmata, etc (probablemente sea fácil de portar a Delphi).

Lo primero de todo, es instalar el paquete FirmataBoard.lpk, está en el directorio FirmataBoard. Una vez instalado, creará una pestaña en la paleta de componentes llamada 'Firmata'.
En estos momentos la versión es la 2.6 firmware 2.12, pero podría funcionar con otra, especialmente con otros ConfigurableFirmata especialmente con protocolo 2.6.

El componente principal es TBoard y es independiente de la fuente de datos. A TBoard se le pueden enlazar diferentes módulos TPin, TTasks (Scheduler), TOneWire, TI2C(en estos momentos sólo uno), TAccelStepper, TAccelStepperGroup, TSerial, TServo, TEncoder, TDht, TFrequency, si hay suficientes pines libres.
Hay además dos módulos más TPS2Mouse y TNeoPixel, no son módulos estándar de ConfigurableFirmata por lo que hay que hacer pequeños cambios en el código de ConfigurableFirmata para que funcionen.

Más importante es modificar el código de ConfiguableFirmata para que Scheduler y Onewire funcionen correctamente en un ESP8266, ESP32 y otros tipos dispositivos, ya que como están sólo funcionan con Arduino.

Se muestrasn algunos ejemplos de uso. Los ejemplos de uso por puerto serie se realizaron en Arduino Uno, ESP8266 y ESP32. Se adjunta también un ejemplo de uso mediante TCP esta prueba se realizó con ESP32.

En este momento es una versión beta del protocolo 2.6, los módulos que no tienen ejemplo es que no se han probado pero deberían funcionar.