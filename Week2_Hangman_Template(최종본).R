# 간단한 hangman game을 만들며 제어문(control statement)의 개념들을 하나씩 익혀봅시다.
# 행맨게임은 주어진 문제에 대해 영어 알파벳을 하나씩 추측하며정답을 맞히는 게임입니다.
# 오답을 5번을 넘기지 않으면서 정답을 맞춰 철자를 맞혀야 합니다.

# 다음 조건들을 생각해봅시다.
#   1. 6번 틀리면 플에이어의 패배, 그 전에 정답을 맞히면 플레이어의 승리!
#   2. 추측은 영어 알파벳으로만.
#   3. 한번 틀린 알파벳은 다시 사용하지 않도록.

# variable dictionary (변수 사전) -----------------------

# number.mistakes:  누적 실수 횟수
# player:           플레이어의 답안지  # e.g. _ _ a _ _ _
# hangman.word:     정답 단어
# letter:           사용자가 추측한 알파벳
# used.letters:     이미 사용된 알파벳

install.packages("stringr")
library(stringr)

play.hangman <- function() {
  #코드 의도: 변수 사전을 정의해 보세요.
  number.mistakes <- 0 # 문제 1. 틀린 횟수는 초기에 몇 번이 되어야 할까요?(언더바__지우고 작성)
  hangman.word <- "korea"
  split.word <- strsplit(hangman.word, '')[[1]]
  used.letters <- c()
  n <- str_length(hangman.word) #stringr 패키지에 있는 함수
  player <- rep('_', n)
  
  #코드 의도: 먼저, 단어를 추측할 때마다 number.mistakes를 확인해야 합니다.
  
  while (number.mistakes<6)  { # 문제2. number.mistakes가 6 미만(<6)까지로 반복문을 작성주세요.(언더바__지우고 작성)
    cat(player, sep = '')  #코드 의도: 먼저 답안지를 출력.
    letter <- "뭐지" # 문제3. 사용자가 추측하도록 유도하는 메시지를 작성해 보세요. ("___"을 지우고 "문구" 작성)
    while(!(toupper(letter) %in% LETTERS)){
      letter <- readline("영어 알파벳으로 추측해보세요.: ") #코드 의도: 자료 유형에 오류가 있을 때
    }
    while(toupper(letter) %in% used.letters) {  #코드 의도: 이미 사용했던 알파벳이라면? 어떤 제어문을 사용할까요?
      guess.messages <- cat(paste("이미 사용된 알파벳입니다.", toupper(letter)))  #코드 의도: 사용자에게 이미 사용했던 알파벳이라고 알려주기!
      letter <- readline(paste(guess.messages, '다른 알파벳으로 추측해보세요: '))
    }
    
    # 1. 'a'를 korea라는 문자열에서 검색
    # 2. a를 c('k', 'o', 'r', 'e', 'a')라는 문자열 벡터에서 검색
    
    if(letter %in% split.word) {  # 알파벳이 답이라면? 어떤 제어문을 쓸까요?
      logic.v <- split.word == letter # 논리형
      player[logic.v] <- letter
      word.display <- paste(player)  # 맞힌 자리에 알파벳을 채워서 답안지를 보여주기
    } else {
            number.mistakes <- number.mistakes+1 # 문제4. 추측이 틀렸다면! number.mistakes에는 어떤 변화가 있을까요?(number.mistakes +? -? 1)(언더바__지우고 작성)
              cat("틀렸습니다! 남은 기회:", 6-number.mistakes, "\n") # 문제5. 플레이어에게 몇 번의 기회(number.mistakes +? -? 1)가 남았는지 알려줘야 합니다.(언더바__지우고 작성)
    }
    used.letters <-c(used.letters, toupper(letter))  # 지금까지 사용된 알파벳의 정보를 저장합니다.
  
  if(!'_' %in% player) {
    cat('승리!')
    break
   }
  }
  if(number.mistakes ==7)  { # 문제6. number.mistakes가 7이 되면 플레이어는 패배한다는 조건문은?(언더바__지우고 작성)
            guess.messages <- " game over message" # 문제7. guess.messages에 game over message를 작성해봅시다.(언더바__지우고 작성)
    cat(guess.messages, " game over message", "\n")# 문제7. guess.messages에 game over message를 작성해봅시다.(언더바__지우고 작성)
  }
}


play.hangman()
