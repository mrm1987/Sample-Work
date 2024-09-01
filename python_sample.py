########################################## PREAMBLE ##########################################

import cv2
import PySimpleGUI as psg
import pandas as pd
import os.path
import random


########################################## INTRO ##########################################

def intro():
    psg.theme('Reddit')
    intro = [[psg.T(text='Welcome to the Higher-Level Cognition Lab!',
                    font=('Arial', 22, 'bold'),
                    size=20,
                    expand_x=True,
                    justification='center')],
             [psg.Multiline(
                 default_text="Your participation in the current time-slot counts towards partial fulfillment "
                              "of the mandatory research participation requirements for PSYCH 110. You will "
                              "have 1-hour to complete the following experiments. Upon completion or at the "
                              "end of the allotted time, you will receive 2 research credits for you "
                              "participation. You will still receive credits even if you fail to complete the "
                              "experiments in the allotted time.\n\n"
                              "Please take a moment and ensure that you have already agreed to and signed the "
                              "consent form provided by research assistants when you entered the lab.\n\n"
                              "If you have any questions or concerns about your responsibilities, please "
                              "notify a research assistant at this point to receive clarification. Keep in "
                              "mind, you may ask for their assistance at any time during your participation.\n\n"
                              "When you're ready to proceed, please input your 3-digit participant ID below.",
                 key='-MLINE-' + psg.WRITE_ONLY_KEY,
                 font=('Arial', 15),
                 expand_x=True,
                 pad=(30, 15),
                 size=(40, 16),
                 text_color='Black',
                 no_scrollbar=True)],
             [psg.T(text="Your 3-digit Participant ID: ",
                    font=('Arial', 15, "bold"),
                    size=30,
                    pad=(30, 10))],
             [psg.Input(default_text='',
                        key='-INPUT_KEY-',
                        font=('Arial', 15),
                        size=(10),
                        pad=(30, 0),
                        enable_events=True)],
             [psg.Push(), psg.Button(button_text='Continue',
                                     disabled=True,
                                     font=('Arial', 15, 'bold'))]]

    win1 = psg.Window('HLC Lab', intro, size=(800, 600))

    while True:
        event, values = win1.read()
        if event == psg.WIN_CLOSED:
            break
        elif event in '-INPUT_KEY-':
            disabled = not (len(values['-INPUT_KEY-']) == 3)
            win1['Continue'].update(disabled=disabled)
        elif event == 'Continue':
            data['ID'] = values.values()
            print(data)
            win1.close()
            instr1()


########################################## INSTRUCTION_1 ##########################################

def instr1():
    psg.theme('Reddit')
    intro1 = [[psg.Multiline(default_text="This experiment is interested in your beliefs about the outcome of "
                                          "wheel-and-spinner games. Your task will be to watch short videos and to "
                                          "respond to the associated question prompts. \n\n"
                                          ""
                                          "Each video is an instance of a hypothetical player's spin. Players win if "
                                          "the spinner lands in the colored sector demarcated as the winning color. "
                                          "All sectors are equally proportioned and are colored either red, blue, "
                                          "or, yellow. An illustration of the wheel and its winning sector is "
                                          "illustrated below.",

                             key='-MLINE-' + psg.WRITE_ONLY_KEY,
                             font=('Arial', 15),
                             expand_x=True,
                             pad=(30, 15),
                             size=(40, 9),
                             text_color='Black',
                             no_scrollbar=True)],
              [psg.Image('blue.png', expand_x=True, subsample=2)],
              [psg.Image('spindir.png', expand_x=True)],
              [psg.Push(), psg.Button(button_text='Continue', font=('Arial', 15, 'bold'))]]

    win2 = psg.Window('HLC Lab', intro1, size=(800, 720), finalize=True)

    while True:
        event, values = win2.read()
        if event == psg.WIN_CLOSED:
            break
        elif event == 'Continue':
            win2.close()
            instr2()


########################################## INSTRUCTION_2 ##########################################

def instr2():
    psg.theme('Reddit')
    intro2 = [[psg.Multiline(default_text='',
                             key='-VARIEDTEXT-',
                             disabled=True,
                             font=('Arial', 15),
                             expand_x=True,
                             pad=(30, 50),
                             size=(40, 20),
                             text_color='Black',
                             no_scrollbar=True)],
              [psg.Push(), psg.Button(button_text='Continue', font=('Arial', 15, 'bold'))]]

    win3 = psg.Window('HLC Lab', intro2, size=(800, 650), finalize=True)

    # Necessary for bolded text in the Multiline element
    win3["-VARIEDTEXT-"].print("HERE ARE YOUR SPECIFIC INSTRUCTIONS:\n\n ",
                               font=('Arial', 18, 'bold'), end='')

    win3["-VARIEDTEXT-"].print("    (1) Watch the video by pressing the 'Play' button in the lower left-hand\n "
                               "          corner of the window (NOTE: You may rewatch the video by pressing\n "
                               "          'Play' a second time)\n\n"
                               ""
                               "     (2) After watching, carefully read the question written below the video.\n\n"
                               "     (3) If the player LOST, answer the question by dragging the slider below\n "
                               "         the prompt. Your response will be recorded after hitting the 'Continue'\n "
                               "         button.\n\n"
                               ""
                               "     (4) If the player WON, select the button below the slider that says 'THIS\n "
                               "         PLAYER WON'. This will take you to the next video.\n\n"
                               ""
                               "This experiment will take approximately 30 minutes to complete. Please watch the "
                               "videos carefully and answer the questions to the best of your ability. When you're "
                               "ready to begin, please proceed by pressing 'Continue'."

                               ,
                               font=('Arial', 15), end='')

    while True:
        event, values = win3.read()
        if event == psg.WIN_CLOSED:
            break
        elif event == 'Continue':
            win3.close()
            wheel(stims[x]['picture'], stims[x]['video'])


########################################## STIMULI ##########################################

def wheel(picture, video):
    global x
    psg.theme('Reddit')
    stim = [[psg.Image(source=picture, expand_x=True, subsample=2)],
            [psg.Image('thumbnail.png', key='image')],
            [psg.Text(text='"This player almost won"', font=('Arial', 18, 'bold'), pad=((0, 0), (10, 5)))],
            [psg.Text(text='On a scale from 1 to 10, how much you agree with this statement?',
                      font=('Arial', 15, 'italic'), pad=((0, 0), (0, 15)))],
            [psg.Text(text='Very much disagree', font=('Arial', 12, 'bold')),
             psg.Push(), psg.Text(text='Very much agree', font=('Arial', 12, 'bold'))],
            [psg.Slider((1, 10), font=('Arial', 18, 'bold'), orientation='horizontal', size=(32, 20),
                        pad=((30, 0), (0, 40)), enable_events=True, key='-SMOVE-')],
            [psg.Button('Play', font=('Arial', 15, 'bold'), disabled=False),
             psg.Button('THIS PLAYER WON', font=('Arial', 15, 'bold'), pad=((110, 0), (0, 0)), disabled=True),
             psg.Push(), psg.Button('Continue', font=('Arial', 15, 'bold'), disabled=True)]]

    win4 = psg.Window('HLC Lab',
                      stim, location=(800, 400), finalize=True)

    cap = cv2.VideoCapture(video)
    playing = False

    while True:
        # Window timemout must be set to 0 in order to achieve correct video FPS
        event, values = win4.read(timeout=0)
        if event == psg.WIN_CLOSED:
            break
        if event == '-SMOVE-':
            win4['Continue'].update(disabled=False)
        if event == 'Play':
            playing = True
            win4['image'].update(source='thumbnail.png')
        if playing is True:
            ret, frame = cap.read()
            if ret:
                # Workaround for getting video to play in PySimpleGUI environment
                imgbytes = cv2.imencode('.png', frame)[1].tobytes()
                win4['image'].update(data=imgbytes)
                win4['Play'].update(disabled=True)
            else:
                # Reactivate the 'Play' button when the video ends
                playing = False
                win4['Play'].update(disabled=False)
                win4['THIS PLAYER WON'].update(disabled=False)
                cap = cv2.VideoCapture(video)
        if event == 'THIS PLAYER WON':
            data[stims[x]['video']] = 'won'
            win4.close()
            x += 1
            if x < 30:
                wheel(stims[x]['picture'], stims[x]['video'])
            else:
                break
        if event == 'Continue':
            # Write slider input to data frame
            data[stims[x]['video']] = values.values()
            win4.close()
            x += 1
            if x < 30:
                wheel(stims[x]['picture'], stims[x]['video'])
            else:
                win4.close()
                outro()


########################################## FINAL SLIDE ##########################################

def outro():
    psg.theme('Reddit')
    outro = [[psg.T(text='Thanks for participating!',
                    font=('Arial', 30, 'bold'),
                    size=20,
                    expand_x=True,
                    justification='center')],
             [psg.Multiline(default_text="If you have any questions or concerns about this experiment, please feel "
                                         "free to reach out to the principle investigator, Matthew Myers "
                                         "(matthewmyers2020@u.northwestern.edu).\n\n"
                                         "When you're ready, please notify a lab assistant that you've completed "
                                         "the 'Wheels' experiment. They'll instruct you on how to proceed.\n\n"
                                         "Have a nice day!",
                            key='-MLINE-' + psg.WRITE_ONLY_KEY,
                            font=('Arial', 15),
                            expand_x=True,
                            pad=(40, 30),
                            size=(40, 9),
                            text_color='Black',
                            no_scrollbar=True)],
             [psg.Button('EXIT PROGRAM', font=('Arial', 15, 'bold'), pad=(300, 0))]]

    win5 = psg.Window('HLC Lab', outro, size=(800, 500), finalize=True)

    while True:
        event, values = win5.read()
        if event == psg.WIN_CLOSED:
            break
        elif event == 'EXIT PROGRAM':
            win5.close()


########################################## STIMULI RANDOMIZATION ##########################################

stim_1 = {'picture': 'yellow.png', 'video': 'fast_distb_close_blue.mp4'}
stim_2 = {'picture': 'blue.png', 'video': 'fast_distb_close_red.mp4'}
stim_3 = {'picture': 'red.png', 'video': 'fast_distb_close_yellow.mp4'}
stim_4 = {'picture': 'yellow.png', 'video': 'fast_distb_far_blue.mp4'}
stim_5 = {'picture': 'blue.png', 'video': 'fast_distb_far_red.mp4'}
stim_6 = {'picture': 'red.png', 'video': 'fast_distb_far_yellow.mp4'}

stim_7 = {'picture': 'red.png', 'video': 'fast_nearb_close_blue.mp4'}
stim_8 = {'picture': 'yellow.png', 'video': 'fast_nearb_close_red.mp4'}
stim_9 = {'picture': 'blue.png', 'video': 'fast_nearb_close_yellow.mp4'}
stim_10 = {'picture': 'red.png', 'video': 'fast_nearb_far_blue.mp4'}
stim_11 = {'picture': 'yellow.png', 'video': 'fast_nearb_far_red.mp4'}
stim_12 = {'picture': 'blue.png', 'video': 'fast_nearb_far_yellow.mp4'}

stim_13 = {'picture': 'yellow.png', 'video': 'slow_distb_close_blue.mp4'}
stim_14 = {'picture': 'blue.png', 'video': 'slow_distb_close_red.mp4'}
stim_15 = {'picture': 'red.png', 'video': 'slow_distb_close_yellow.mp4'}
stim_16 = {'picture': 'yellow.png', 'video': 'slow_distb_far_blue.mp4'}
stim_17 = {'picture': 'blue.png', 'video': 'slow_distb_far_red.mp4'}
stim_18 = {'picture': 'red.png', 'video': 'slow_distb_far_yellow.mp4'}

stim_19 = {'picture': 'red.png', 'video': 'slow_nearb_close_blue.mp4'}
stim_20 = {'picture': 'yellow.png', 'video': 'slow_nearb_close_red.mp4'}
stim_21 = {'picture': 'blue.png', 'video': 'slow_nearb_close_yellow.mp4'}
stim_22 = {'picture': 'red.png', 'video': 'slow_nearb_far_blue.mp4'}
stim_23 = {'picture': 'yellow.png', 'video': 'slow_nearb_far_red.mp4'}
stim_24 = {'picture': 'blue.png', 'video': 'slow_nearb_far_yellow.mp4'}

stim_25 = {'picture': 'blue.png', 'video': 'filler_2.5x_50_blue.mp4'}
stim_26 = {'picture': 'red.png', 'video': 'filler_2.5x_50_red.mp4'}
stim_27 = {'picture': 'yellow.png', 'video': 'filler_2.5x_50_yellow.mp4'}
stim_28 = {'picture': 'blue.png', 'video': 'filler_2.5x_70_blue.mp4'}
stim_29 = {'picture': 'red.png', 'video': 'filler_2.5x_70_red.mp4'}
stim_30 = {'picture': 'yellow.png', 'video': 'filler_2.5x_70_yellow.mp4'}

stims = [stim_1, stim_2, stim_3, stim_4, stim_5, stim_6, stim_7, stim_8, stim_9, stim_10, stim_11, stim_12, stim_13,
         stim_14, stim_15, stim_16, stim_17, stim_18, stim_19, stim_20, stim_21, stim_22, stim_23, stim_24, stim_25,
         stim_26, stim_27, stim_28, stim_29, stim_30]

random.shuffle(stims)

x = 0

########################################## SEQUENCE START ##########################################

data = {}

intro()

########################################## WRITING/MODIFYING DATA ##########################################

myKeys = list(data.keys())
myKeys.sort()
sorted_data = {i: data[i] for i in myKeys}

df = pd.DataFrame.from_dict(sorted_data)

if os.path.isfile('./wheels_data.csv') is True:
    df.to_csv('wheels_data.csv', mode='a', index=False, header=False)

else:
    df.to_csv('wheels_data.csv', index=False)
