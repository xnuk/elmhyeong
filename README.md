## 빌드 방법

```bash

yarn
yarn webpack

# yarn webpack이 동작하지 않을 경우
rm -rf node_modules/elm && yarn add elm

# 테스트 케이스 검사
yarn elm-test

```

이러면 `dist` 폴더에 빌드 결과물이 저장되게 됩니다.

## 한계점
- 분모 또는 분자의 절댓값이 2의 31제곱 이상인 경우 오버플로우/언더플로우 발생
