; This is gambit scheme code using Gambit 93f8107

(import (scheme base))

(c-declare
#<<c-declare-end

 #include <soundio/soundio.h>

 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 //#include <math.h>


static float get_sample(float);

static float seconds_offset = 0.0f;

static void write_callback(struct SoundIoOutStream *outstream,
        int frame_count_min, int frame_count_max)
{
    const struct SoundIoChannelLayout *layout = &outstream->layout;
    float float_sample_rate = outstream->sample_rate;
    float seconds_per_frame = 1.0f / float_sample_rate;
    struct SoundIoChannelArea *areas;
    int frames_left = frame_count_max;
    int err;

    while (frames_left > 0) {
        int frame_count = frames_left;

        if ((err = soundio_outstream_begin_write(outstream, &areas, &frame_count))) {
            fprintf(stderr, "%s\n", soundio_strerror(err));
            exit(1);
        }

        if (!frame_count)
            break;

        //float pitch = 440.0f;
        //float radians_per_second = pitch * 2.0f * PI;
        for (int frame = 0; frame < frame_count; frame += 1) {
            //float sample = sin((seconds_offset + frame * seconds_per_frame) * radians_per_second);
            for (int channel = 0; channel < layout->channel_count; channel += 1) {
                float *ptr = (float*)(areas[channel].ptr + areas[channel].step * frame);
                *ptr = get_sample(seconds_per_frame);
            }
        }

        if ((err = soundio_outstream_end_write(outstream))) {
            fprintf(stderr, "%s\n", soundio_strerror(err));
            exit(1);
        }

        frames_left -= frame_count;
    }
}

int play(void) {
    int err;
    struct SoundIo *soundio = soundio_create();
    if (!soundio) {
        fprintf(stderr, "out of memory\n");
        return 1;
    }

    if ((err = soundio_connect(soundio))) {
        fprintf(stderr, "error connecting: %s\n", soundio_strerror(err));
        return 1;
    }

    soundio_flush_events(soundio);

    int default_out_device_index = soundio_default_output_device_index(soundio);
    if (default_out_device_index < 0) {
        fprintf(stderr, "no output device found\n");
        return 1;
    }

    struct SoundIoDevice *device = soundio_get_output_device(soundio, default_out_device_index);
    if (!device) {
        fprintf(stderr, "out of memory\n");
        return 1;
    }

    fprintf(stderr, "Output device: %s\n", device->name);

    struct SoundIoOutStream *outstream = soundio_outstream_create(device);
    if (!outstream) {
        fprintf(stderr, "out of memory\n");
        return 1;
    }
    outstream->format = SoundIoFormatFloat32NE;
    outstream->write_callback = write_callback;

    if ((err = soundio_outstream_open(outstream))) {
        fprintf(stderr, "unable to open device: %s", soundio_strerror(err));
        return 1;
    }

    if (outstream->layout_error)
        fprintf(stderr, "unable to set channel layout: %s\n", soundio_strerror(outstream->layout_error));

    if ((err = soundio_outstream_start(outstream))) {
        fprintf(stderr, "unable to start device: %s\n", soundio_strerror(err));
        return 1;
    }

    for (;;)
        soundio_wait_events(soundio);

    soundio_outstream_destroy(outstream);
    soundio_device_unref(device);
    soundio_destroy(soundio);
    return 0;
}

c-declare-end
)

(define-record-type channel
 (channel freq current-offset wavegen volume)
 channel?
 (freq channel-freq set-channel-freq!)
 (current-offset channel-offset set-channel-offset!)
 (wavegen channel-wavegen)
 (volume channel-volume set-channel-volume!))

(define pi (acos -1))

(define (sinewave freq offset)
 (sin (* 2 pi freq offset)))

(c-define (get-sample offset) (float) float "get_sample" "static"
 (if (pair? rows)
  (let [[row (car rows)]]
   (if (pair? row)
    (begin
     (if (> (car row) 0)
      (set-car! row (- (car row) 1))
      (set! rows (cdr rows)))
     (let* [[channels (cdr row)]
            [len  (/ 1 (vector-length channels))]]
      (vector-fold + 0
       (vector-map (lambda (ch)
                    (let* [[freq (channel-freq ch)]
                           [addup-offset (+ (channel-offset ch) offset)]
                           [current-offset (- addup-offset (* freq (truncate (/ addup-offset freq))))]]  ; (fmod addup-offset freq)
                     (set-channel-offset! ch current-offset)
                     (* (channel-volume ch) ((channel-wavegen ch) freq current-offset) len))) channels))))
    (begin
     (make-thread add-notes "add notes")
     (set! rows (cdr rows))
     (get-sample offset))))
  0.0))

(define rows (list (cons 20000 (vector (channel 440 0 sinewave 1)))))

((c-lambda () void "play();"))
