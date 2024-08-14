
```markdown
# Executando um Script Haskell no Ubuntu

Este guia fornece instruções simples sobre como rodar um script Haskell no terminal no Ubuntu.

## Pré-requisitos

Antes de começar, você precisa ter o GHC (Glasgow Haskell Compiler) instalado no seu sistema. Você pode instalar o GHC usando o seguinte comando:

```bash
sudo apt update
sudo apt install ghc
```

## Passos para Executar o Script

1. **Crie um Arquivo Haskell:**

   Crie um arquivo com a extensão `.hs` e escreva seu código Haskell nele. Por exemplo, crie um arquivo chamado `hello.hs` com o seguinte conteúdo:

   ```haskell
   main :: IO ()
   main = putStrLn "Olá, Mundo!"
   ```

2. **Compilar o Script:**

   Para compilar o script Haskell, use o comando `ghc` seguido do nome do arquivo. Isso criará um executável no mesmo diretório:

   ```bash
   ghc hello.hs
   ```

3. **Executar o Script Compilado:**

   Após a compilação, você verá um arquivo executável chamado `hello`. Execute o script compilado usando o seguinte comando:

   ```bash
   ./hello
   ```

   A saída será:

   ```bash
   Olá, Mundo!
   ```

4. **Executar o Script Diretamente com GHCi:**

   Se você não quiser compilar o script, pode executá-lo diretamente no GHCi (GHC Interactive) usando o comando `runghc`:

   ```bash
   runghc hello.hs
   ```

   A saída será a mesma:

   ```bash
   Olá, Mundo!
   ```

## Usando GHCi para Carregar e Recarregar Scripts

GHCi é o interpretador interativo de Haskell, que permite testar código rapidamente e carregar scripts.

1. **Iniciar GHCi:**

   Abra o terminal e digite `ghci` para iniciar o interpretador interativo:

   ```bash
   ghci
   ```

2. **Carregar um Script:**

   Use o comando `:l` seguido do nome do arquivo para carregar um script em GHCi:

   ```haskell
   :l hello.hs
   ```

   Após carregar o script, você pode chamar a função `main`:

   ```haskell
   main
   ```

   A saída será:

   ```haskell
   Olá, Mundo!
   ```

3. **Recarregar um Script:**

   Se você fizer alterações no arquivo Haskell, pode recarregar o script no GHCi usando o comando `:r`:

   ```haskell
   :r
   ```

   Isso recarregará o arquivo e aplicará as alterações feitas.

## Recursos Adicionais

- [Documentação Oficial do GHCI](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html))
- [Haskell.org](https://www.haskell.org/)

---

Siga esses passos para rodar seus scripts Haskell no terminal no Ubuntu. Se você tiver qualquer dúvida ou encontrar algum problema, consulte a documentação oficial ou procure ajuda na comunidade Haskell.
```

Este README agora inclui instruções sobre como usar GHCi para carregar e recarregar scripts Haskell, além das instruções anteriores para compilar e executar scripts.
